import { Link, useParams } from 'react-router-dom'
export default function Sidebar() {
    const { treeId } = useParams()
    return (
        <aside className="w-60 p-3 text-ivory space-y-2">
            <Link className="block btn-outline text-left" to="/app">My Trees</Link>
            {treeId && (
                <>
                    <Link className="block btn-outline text-left" to={`/app/trees/${treeId}/members`}>Members</Link>
                    <Link className="block btn-outline text-left" to={`/app/trees/${treeId}/relations`}>Relations</Link>
                    <Link className="block btn-outline text-left" to={`/app/trees/${treeId}/graph`}>Graph</Link>
                    <Link className="block btn-outline text-left" to={`/app/trees/${treeId}/share`}>Share</Link>
                    <Link className="block btn-outline text-left" to={`/app/trees/${treeId}/audit`}>Audit</Link>
                </>
            )}
        </aside>
    )
}
