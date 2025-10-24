// src/pages/admin/FamilyTreesPage.tsx
import React, { useEffect, useState } from 'react';
import { adminApi, FamilyTree } from '@/api/ts_admin';

const FamilyTreesPage: React.FC = () => {
    const [familyTrees, setFamilyTrees] = useState<FamilyTree[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [searchTerm, setSearchTerm] = useState('');
    const [showPublicOnly, setShowPublicOnly] = useState<boolean | null>(null);

    useEffect(() => {
        fetchFamilyTrees();
    }, []);

    const fetchFamilyTrees = async () => {
        try {
            setLoading(true);
            setError(null);

            console.log('🔄 Starting to fetch family trees...');
            const trees = await adminApi.getAllFamilyTrees();

            console.log('✅ Family trees loaded:', trees);
            setFamilyTrees(trees || []);
        } catch (err: any) {
            const errorMsg = err.response?.data?.message || err.message || 'Unknown error';
            console.error('❌ Error fetching family trees:', err);
            setError(errorMsg);
        } finally {
            setLoading(false);
        }
    };

    const formatDate = (dateString: string) => {
        return new Date(dateString).toLocaleDateString('en-US', {
            year: 'numeric',
            month: 'short',
            day: 'numeric'
        });
    };

    // Filter logic
    const filteredTrees = familyTrees.filter(tree => {
        const matchesSearch = searchTerm === '' ||
            tree.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
            tree.createdByEmail?.toLowerCase().includes(searchTerm.toLowerCase()) ||
            tree.createdByUsername?.toLowerCase().includes(searchTerm.toLowerCase());

        const matchesVisibility = showPublicOnly === null || tree.isPublic === showPublicOnly;

        return matchesSearch && matchesVisibility;
    });

    if (loading) {
        return (
            <div className="flex items-center justify-center min-h-screen">
                <div className="text-center">
                    <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
                    <p className="mt-4 text-gray-600">Loading family trees...</p>
                </div>
            </div>
        );
    }

    if (error) {
        return (
            <div className="bg-red-50 border border-red-200 rounded-lg p-6 m-8">
                <div className="flex items-center">
                    <span className="text-2xl mr-3">⚠️</span>
                    <div>
                        <h3 className="text-lg font-semibold text-red-800">Error Loading Family Trees</h3>
                        <p className="text-red-600 mt-1">{error}</p>
                        <button
                            onClick={fetchFamilyTrees}
                            className="mt-3 px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700 transition-colors"
                        >
                            Try Again
                        </button>
                    </div>
                </div>
            </div>
        );
    }

    return (
        <div className="p-8">
            {/* Header */}
            <div className="flex justify-between items-center mb-6">
                <div>
                    <h1 className="text-3xl font-bold text-gray-900 flex items-center">
                        <span className="text-4xl mr-3">🌳</span>
                        Family Trees Management
                    </h1>
                    <p className="text-gray-600 mt-2">
                        View and manage all family trees in the system
                    </p>
                </div>
                <button
                    onClick={fetchFamilyTrees}
                    className="flex items-center px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
                >
                    <span className="mr-2">🔄</span>
                    Refresh
                </button>
            </div>

            {/* Stats Cards */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
                <div className="bg-white rounded-lg shadow-sm border p-6">
                    <div className="flex items-center justify-between">
                        <div>
                            <p className="text-gray-600 text-sm">Total Trees</p>
                            <p className="text-3xl font-bold text-gray-900 mt-1">{familyTrees.length}</p>
                        </div>
                        <div className="text-4xl">🌳</div>
                    </div>
                </div>

                <div className="bg-white rounded-lg shadow-sm border p-6">
                    <div className="flex items-center justify-between">
                        <div>
                            <p className="text-gray-600 text-sm">Public Trees</p>
                            <p className="text-3xl font-bold text-green-600 mt-1">
                                {familyTrees.filter(t => t.isPublic).length}
                            </p>
                        </div>
                        <div className="text-4xl">🌍</div>
                    </div>
                </div>

                <div className="bg-white rounded-lg shadow-sm border p-6">
                    <div className="flex items-center justify-between">
                        <div>
                            <p className="text-gray-600 text-sm">Private Trees</p>
                            <p className="text-3xl font-bold text-orange-600 mt-1">
                                {familyTrees.filter(t => !t.isPublic).length}
                            </p>
                        </div>
                        <div className="text-4xl">🔒</div>
                    </div>
                </div>
            </div>

            {/* Search and Filter Bar */}
            <div className="bg-white rounded-lg shadow-sm border p-4 mb-6">
                <div className="flex flex-col md:flex-row gap-4">
                    {/* Search Input */}
                    <div className="flex-1">
                        <input
                            type="text"
                            placeholder="🔍 Search by tree name, email, or username..."
                            value={searchTerm}
                            onChange={(e) => setSearchTerm(e.target.value)}
                            className="w-full px-4 py-2 border rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
                        />
                    </div>

                    {/* Visibility Filter */}
                    <div className="flex gap-2">
                        <button
                            onClick={() => setShowPublicOnly(null)}
                            className={`px-4 py-2 rounded-lg transition-colors ${
                                showPublicOnly === null
                                    ? 'bg-blue-600 text-white'
                                    : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                            }`}
                        >
                            All
                        </button>
                        <button
                            onClick={() => setShowPublicOnly(true)}
                            className={`px-4 py-2 rounded-lg transition-colors ${
                                showPublicOnly === true
                                    ? 'bg-green-600 text-white'
                                    : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                            }`}
                        >
                            🌍 Public
                        </button>
                        <button
                            onClick={() => setShowPublicOnly(false)}
                            className={`px-4 py-2 rounded-lg transition-colors ${
                                showPublicOnly === false
                                    ? 'bg-orange-600 text-white'
                                    : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                            }`}
                        >
                            🔒 Private
                        </button>
                    </div>
                </div>

                {/* Results count */}
                {searchTerm || showPublicOnly !== null ? (
                    <div className="mt-3 text-sm text-gray-600">
                        Showing {filteredTrees.length} of {familyTrees.length} trees
                    </div>
                ) : null}
            </div>

            {/* Trees List */}
            {filteredTrees.length === 0 ? (
                <div className="bg-gray-50 border-2 border-dashed border-gray-300 rounded-lg p-12 text-center">
                    <div className="text-6xl mb-4">🌱</div>
                    <h3 className="text-xl font-semibold text-gray-700 mb-2">
                        {familyTrees.length === 0 ? 'No Family Trees Yet' : 'No Trees Match Your Filter'}
                    </h3>
                    <p className="text-gray-500">
                        {familyTrees.length === 0
                            ? 'Family trees will appear here once users create them.'
                            : 'Try adjusting your search or filter criteria.'
                        }
                    </p>
                </div>
            ) : (
                <div className="bg-white rounded-lg shadow-sm border overflow-hidden">
                    <table className="min-w-full divide-y divide-gray-200">
                        <thead className="bg-gray-50">
                        <tr>
                            <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                                Tree Name
                            </th>
                            <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                                Created By
                            </th>
                            <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                                Visibility
                            </th>
                            <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                                Created At
                            </th>
                            <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                                Actions
                            </th>
                        </tr>
                        </thead>
                        <tbody className="bg-white divide-y divide-gray-200">
                        {filteredTrees.map((tree) => (
                            <tr key={tree.id} className="hover:bg-gray-50 transition-colors">
                                <td className="px-6 py-4 whitespace-nowrap">
                                    <div className="flex items-center">
                                        <div className="flex-shrink-0 h-10 w-10 bg-green-100 rounded-lg flex items-center justify-center">
                                            <span className="text-xl">🌳</span>
                                        </div>
                                        <div className="ml-4">
                                            <div className="text-sm font-medium text-gray-900">
                                                {tree.name}
                                            </div>
                                            {tree.description && (
                                                <div className="text-sm text-gray-500 truncate max-w-xs">
                                                    {tree.description}
                                                </div>
                                            )}
                                        </div>
                                    </div>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap">
                                    <div className="text-sm text-gray-900">
                                        {tree.createdByUsername || 'Unknown'}
                                    </div>
                                    <div className="text-sm text-gray-500">
                                        {tree.createdByEmail || 'No email'}
                                    </div>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap">
                                        <span className={`px-2 inline-flex text-xs leading-5 font-semibold rounded-full ${
                                            tree.isPublic
                                                ? 'bg-green-100 text-green-800'
                                                : 'bg-gray-100 text-gray-800'
                                        }`}>
                                            {tree.isPublic ? '🌍 Public' : '🔒 Private'}
                                        </span>
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                                    {formatDate(tree.createdAt)}
                                </td>
                                <td className="px-6 py-4 whitespace-nowrap text-sm font-medium">
                                    <button
                                        className="text-blue-600 hover:text-blue-900 mr-3 transition-colors"
                                        onClick={() => console.log('View tree:', tree.id)}
                                    >
                                        View
                                    </button>
                                    <button
                                        className="text-red-600 hover:text-red-900 transition-colors"
                                        onClick={() => {
                                            if (confirm(`Are you sure you want to delete "${tree.name}"?`)) {
                                                console.log('Delete tree:', tree.id);
                                            }
                                        }}
                                    >
                                        Delete
                                    </button>
                                </td>
                            </tr>
                        ))}
                        </tbody>
                    </table>
                </div>
            )}
        </div>
    );
};

export default FamilyTreesPage;